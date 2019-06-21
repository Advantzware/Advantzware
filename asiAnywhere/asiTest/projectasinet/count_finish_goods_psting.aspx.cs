
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

public partial class Count_finish_goods_psting : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        //if (Session["User"] != null)
        //{
        //    string vUserId = UserLogin.UserName;
        //    string vPage = "Count_finish_goods_psting.aspx";
        //    string aUsers = null;
        //    string PrmComp = null;
        //    bool vCanCreate = false;
        //    bool vCanRun = false;
        //    bool vCanUpdate = false;
        //    bool vCanDelete = false;

        //    func1 f1 = new func1();

        //    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

        //    labelcompany.Text = PrmComp;
        //    Session["Customers_Company"] = labelcompany.Text;
        //    if (aUsers == "external")
        //    {
        //        //Image14.Visible = false;
        //    }
        //    if (vCanRun == false)
        //    {
        //        Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
        //        Response.Write("<script>window.location.href = 'login.aspx';</script>");

        //    }
        //}

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'Count_finish_goods_psting.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBox1.Text = dr["field1"].ToString();
                    TextBox2.Text = dr["field2"].ToString();
                    TextBox3.Text = dr["field3"].ToString();
                    TextBox4.Text = dr["field4"].ToString();
                    TextBox5.Text = dr["field5"].ToString();
                    
                   
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

                TextBox5.Text = System.DateTime.Today.ToShortDateString();
                RadioButtonList_out.SelectedIndex = 0;                
            }
            catch { }


            
            //if (Session["User"] != null)
            //{
            //    lblUser.Text = UserLogin.UserName;
            //}
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

        if (HiddenFieldPost.Value == "Yes")
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
            ObjectDataSource1.SelectParameters["prmtrnspost"].DefaultValue = "TransPost";
            ObjectDataSource1.SelectParameters["prmBeginTag"].DefaultValue = TextBox1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEndTag"].DefaultValue = TextBox2.Text.Trim(); ;
            ObjectDataSource1.SelectParameters["prmBeginUsrid"].DefaultValue = TextBox3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmEndUsrid"].DefaultValue = TextBox4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPstDate"].DefaultValue = TextBox5.Text.Trim();
            ObjectDataSource1.SelectParameters["prmGlActNm"].DefaultValue = HiddenField1.Value;
            ObjectDataSource1.SelectParameters["prmshwinv"].DefaultValue = HiddenField2.Value;

            ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;





            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'Count_finish_goods_psting.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, chk_field1, chk_field2) values ('" + UserLogin.UserName + "','Count_finish_goods_psting.aspx' , '" + TextBox1.Text.Trim() + "', '" + TextBox2.Text.Trim() + "','" + TextBox3.Text.Trim() + "','" + TextBox4.Text.Trim() + "','" + TextBox5.Text.Trim() + "','" + CheckBox1.Checked + "','" + CheckBox2.Checked + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + TextBox3.Text.Trim() + "', field4 = '" + TextBox4.Text.Trim() + "', field5 = '" + TextBox5.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'count_finish_goods_psting.aspx' ", conn);
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
                Label vpath = (Label)FormView1.FindControl("vtrnspostLabel");
                HyperLink1.Text = vpath.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


                if (vpath.Text == "")
                {
                    OutPutFile.Text = "No CSV Exists";
                    Response.Write("<script>window.location.href='finish_goods_psting.aspx';</script>");
                }
            }
            catch { }
        }
    }
    
}

