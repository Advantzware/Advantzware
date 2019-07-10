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

public partial class ap_chk_reg : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "ap_chk_reg.aspx";
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ap_chk_reg.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    postdateTextBox.Text  = dr["field1"].ToString();
                    perTextBox.Text       = dr["field2"].ToString();
                    chkfileTextBox.Text   = dr["field3"].ToString();

                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;

                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;

                    if (dr["chk_field3"].ToString() == "True")
                    {
                        CheckBox3.Checked = true;
                    }
                    else
                    {
                        CheckBox3.Checked = false;
                        chkfileTextBox.Visible = false;
                    }

                    if (dr["rd_field1"].ToString() == "Manual")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Automatic")
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
                RadioButtonList_out.SelectedIndex = 0;
                RadioButtonList1.SelectedIndex = 0;
            }
            catch { }
            try
            {                
                voucherpay chkfile = new voucherpay();
                DataSet db = new DataSet();
                db = chkfile.SelectAPChkRegister(UserLogin.UserName, "view", "", "", 0, "", "", "Yes", "", "", "");

                chkfileTextBox.Text = Convert.ToString(db.Tables[0].Rows[0][1]);
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
        if (CheckBox3.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        }

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField4.Value = "manual";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField4.Value = "automatic";

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

         voucherpay val = new voucherpay();

         bool check = val.validateAPChkRegister(UserLogin.UserName, "validatechkreg", postdateTextBox.Text.Trim(), Convert.ToString(HiddenField2.Value), Convert.ToInt32(perTextBox.Text.Trim()), Convert.ToString(HiddenField1.Value), Convert.ToString(chkfileTextBox.Text.Trim()), Convert.ToString(HiddenField3.Value), Convert.ToString(HiddenField4.Value), Convert.ToString(RadioButtonList_out.SelectedValue), Convert.ToString(HiddenFieldPost.Value));

        string value = Convert.ToString(check);
        if (value == "True")
        {

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmchkreg"].DefaultValue = "chkreg";
            ObjectDataSource1.SelectParameters["prmPstDate"].DefaultValue = postdateTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmvoidskip"].DefaultValue = HiddenField2.Value;
            ObjectDataSource1.SelectParameters["prmperiod"].DefaultValue = perTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmprtacc"].DefaultValue = HiddenField1.Value;
            ObjectDataSource1.SelectParameters["prmapchkfile"].DefaultValue = HiddenField3.Value;
            ObjectDataSource1.SelectParameters["prmpstmnl"].DefaultValue = HiddenField4.Value;
            ObjectDataSource1.SelectParameters["prmpost"].DefaultValue = Convert.ToString(HiddenFieldPost.Value);
            ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;
            ObjectDataSource1.SelectParameters["prmchkfile"].DefaultValue = chkfileTextBox.Text.Trim();



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ap_chk_reg.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, chk_field1, chk_field2, chk_field3, rd_field1) values ('" + UserLogin.UserName + "','ap_chk_reg.aspx' , '" + postdateTextBox.Text.Trim() + "', '" + perTextBox.Text.Trim() + "', '" + chkfileTextBox.Text.Trim() + "','" + CheckBox1.Checked + "','" + CheckBox2.Checked + "','" + CheckBox3.Checked + "','" + RadioButtonList1.SelectedValue + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + postdateTextBox.Text.Trim() + "', field2 = '" + perTextBox.Text.Trim() + "', field3 = '" + chkfileTextBox.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "', chk_field3 = '" + CheckBox3.Checked + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'ap_chk_reg.aspx' ", conn);
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
                Label vpath = (Label)FormView1.FindControl("vchkregLabel");
                HyperLink1.Text = vpath.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


                if (vpath.Text == "")
                {
                    OutPutFile.Text = "No CSV Exists";
                    Response.Write("<script>window.location.href='ap_chk_reg.aspx';</script>");

                }
            }
            catch { }
        }
    }

    protected void checked_change(object sender, EventArgs e)
    {
        if (CheckBox3.Checked)
        {
            chkfileTextBox.Visible = true;
        }
        else
            chkfileTextBox.Visible = false;
    }

   }
