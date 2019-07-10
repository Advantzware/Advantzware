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

public partial class prnt_ap_dis : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "prnt_ap_dis.aspx";
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'prnt_ap_dis.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    postdateTextBox.Text  = dr["field1"].ToString();
                    perTextBox.Text       = dr["field2"].ToString();
                    begdateTextBox.Text   = dr["field3"].ToString();
                    enddateTextBox.Text   = dr["field4"].ToString();




                    if (dr["rd_field1"].ToString() == "vendor#")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "sequence")
                        RadioButtonList1.SelectedIndex = 1;  
                    
                             
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
                

                
                postdateTextBox.Text = System.DateTime.Today.ToShortDateString();                
                perTextBox.Text = System.DateTime.Today.Month.ToString();
                
                postdateTextBox.Text = System.DateTime.Now.Date.ToShortDateString();                
                RadioButtonList1.SelectedIndex = 0;
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
        

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "vendor#";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "sequence";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

         voucherpay val = new voucherpay();

         bool check = val.ValidateDisbursementRegister(UserLogin.UserName, "validateapdis", postdateTextBox.Text.Trim(), Convert.ToString(begdateTextBox.Text.Trim()), Convert.ToString(enddateTextBox.Text.Trim()), Convert.ToString(HiddenField1.Value), Convert.ToInt32(perTextBox.Text.Trim()), Convert.ToString(HiddenFieldPost.Value));

        string value = Convert.ToString(check);
        if (value == "True")
        {

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmapdis"].DefaultValue = "apdis";
            ObjectDataSource1.SelectParameters["prmpstdate"].DefaultValue = postdateTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmbegdate"].DefaultValue = begdateTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmenddate"].DefaultValue = enddateTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmvndseq"].DefaultValue = HiddenField1.Value;
            ObjectDataSource1.SelectParameters["prmperiod"].DefaultValue = perTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = Convert.ToString(HiddenFieldPost.Value);
            



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'prnt_ap_dis.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, rd_field1) values ('" + UserLogin.UserName + "','prnt_ap_dis.aspx' , '" + postdateTextBox.Text.Trim() + "', '" + perTextBox.Text.Trim() + "', '" + begdateTextBox.Text.Trim() + "', '" + enddateTextBox.Text.Trim() + "', '" + RadioButtonList1.SelectedValue + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + postdateTextBox.Text.Trim() + "', field2 = '" + perTextBox.Text.Trim() + "', field3 = '" + begdateTextBox.Text.Trim() + "', field4 = '" + enddateTextBox.Text.Trim() + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'prnt_ap_dis.aspx' ", conn);
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
                Label vpath = (Label)FormView1.FindControl("apdisLabel");
                HyperLink1.Text = vpath.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


                if (vpath.Text == "")
                {                    
                    OutPutFile.Text = "No File Exists";
                    //Response.Write("<script>window.location.href='prnt_ap_dis.aspx';</script>");

                }
            }
            catch { }
        }
    }

    

   }
