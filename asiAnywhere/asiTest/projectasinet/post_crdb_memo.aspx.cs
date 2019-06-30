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

public partial class post_crdb_memo : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "post_crdb_memo.aspx";
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'post_crdb_memo.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    postdateTextBox.Text       = dr["field1"].ToString();
                    perTextBox.Text       = dr["field2"].ToString();
                    becustTextBox.Text       = dr["field3"].ToString();
                    endcustTextBox.Text       = dr["field4"].ToString();
                    bemmoTextBox.Text       = dr["field5"].ToString();
                    endmmoTextBox.Text       = dr["field6"].ToString();
                    
                    
                    

                    if (dr["chk_field1"].ToString() == "yes")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;                   

                                 
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
                

                if (endcustTextBox.Text == "")
                    endcustTextBox.Text = "zzzzzzzz";
                
                postdateTextBox.Text = System.DateTime.Today.ToShortDateString();
                perTextBox.Text = System.DateTime.Today.Month.ToString();
                
                postdateTextBox.Text = System.DateTime.Now.Date.ToShortDateString();
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
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (CheckBox1.Checked)
            HiddenField1.Value = "yes";
        else
            HiddenField1.Value = "no";
        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "pstcd";
        ObjectDataSource1.SelectParameters["prmbegcust"].DefaultValue = becustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendcust"].DefaultValue = endcustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegdt"].DefaultValue = bemmoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmenddt"].DefaultValue = endmmoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtrnsdt"].DefaultValue = postdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmexprt"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        ObjectDataSource1.SelectParameters["prmperiod"].DefaultValue = perTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = Convert.ToString(HiddenFieldPost.Value); 
        

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'post_crdb_memo.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, chk_field1) values ('" + UserLogin.UserName + "','post_crdb_memo.aspx' , '" + postdateTextBox.Text.Trim() + "', '" + perTextBox.Text.Trim() + "', '" + becustTextBox.Text.Trim() + "', '" + endcustTextBox.Text.Trim() + "', '" + bemmoTextBox.Text.Trim() + "', '" + endmmoTextBox.Text.Trim() + "', '" + HiddenField1.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + postdateTextBox.Text.Trim() + "', field2 = '" + perTextBox.Text.Trim() + "', field3 = '" + becustTextBox.Text.Trim() + "', field4 = '" + endcustTextBox.Text.Trim() + "', field5 = '" + bemmoTextBox.Text.Trim() + "', field6 = '" + endmmoTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'post_crdb_memo.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("pstcdLabel");            
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No File Exists";

            }
        }
        catch { }      
        
    }

   }
