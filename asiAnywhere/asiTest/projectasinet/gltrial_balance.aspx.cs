#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class gltrial_balance : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "gltrial_balance.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = PrmComp;
            if (aUsers == "external")
            {
               
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
        
            if (Session["User"] != null)
            {                
                lblUser.Text = UserLogin.UserName;
            }
            

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'gltrial_balance.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    postdateTextBox.Text  = dr["field1"].ToString();
                    perTextBox.Text       = dr["field2"].ToString();
                    begactTextBox.Text    = dr["field3"].ToString();
                    endactTextBox.Text    = dr["field4"].ToString();
                    subactTextBox.Text    = dr["field5"].ToString();
                    begsubTextBox.Text    = dr["field6"].ToString();
                    endsubTextBox.Text    = dr["field7"].ToString();


                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;

                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;  
                    
                             
                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {

                if (begsubTextBox.Text == "")
                    begsubTextBox.Text = "0";
                if (endsubTextBox.Text == "")
                    endsubTextBox.Text = "999999999"; 

                
                postdateTextBox.Text = System.DateTime.Today.ToShortDateString();                
                perTextBox.Text = System.DateTime.Today.Month.ToString();
                
                postdateTextBox.Text = System.DateTime.Now.Date.ToShortDateString();
                RadioButtonList_out.SelectedIndex = 0;
            }
            catch { }
            
            
            
        }
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        if (CheckBox1.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (CheckBox2.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        } 

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];         

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "tribal";
            ObjectDataSource1.SelectParameters["prmtrnsdate"].DefaultValue = postdateTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmperiod"].DefaultValue = perTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmbegact"].DefaultValue = begactTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmbegsub"].DefaultValue = begsubTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmendact"].DefaultValue = endactTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmendsub"].DefaultValue = endsubTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmZerobal"].DefaultValue = HiddenField1.Value;
            ObjectDataSource1.SelectParameters["prmsortsub"].DefaultValue = HiddenField2.Value;
            ObjectDataSource1.SelectParameters["prmactlevl"].DefaultValue = subactTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;
            



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'gltrial_balance.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, chk_field1, chk_field2) values ('" + UserLogin.UserName + "','gltrial_balance.aspx' , '" + postdateTextBox.Text.Trim() + "', '" + perTextBox.Text.Trim() + "', '" + begactTextBox.Text.Trim() + "', '" + endactTextBox.Text.Trim() + "', '" + subactTextBox.Text.Trim() + "', '" + begsubTextBox.Text.Trim() + "', '" + endsubTextBox.Text.Trim() + "' ,  '" + CheckBox1.Checked + "', '" + CheckBox2.Checked + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + postdateTextBox.Text.Trim() + "', field2 = '" + perTextBox.Text.Trim() + "', field3 = '" + begactTextBox.Text.Trim() + "', field4 = '" + endactTextBox.Text.Trim() + "', field5 = '" + subactTextBox.Text.Trim() + "', field6 = '" + begsubTextBox.Text.Trim() + "', field7 = '" + endsubTextBox.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'gltrial_balance.aspx' ", conn);
                    cmd_update.ExecuteNonQuery();
                }
                conn.Close();
            }
            catch (Exception ex)
            {
                Label1.Text = "Error :" + ex.Message + "<p>";
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {

                OutPutFile.Visible = true;
                HyperLink1.Visible = true;
                Label vpath = (Label)FormView1.FindControl("tribalLabel");
                HyperLink1.Text = vpath.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


                if (vpath.Text == "")
                {
                    OutPutFile.Text = "No CSV Exists";
                    Response.Write("<script>window.location.href='gltrial_balance.aspx';</script>");
                }
            }
            catch { }  
        }
    

    

   }
