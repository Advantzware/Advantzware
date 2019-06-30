using System;
using System.Collections.Generic;
using System.Linq;
using System.Configuration;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Threading;
using System.Globalization;

    public partial class act_dbcrlist : System.Web.UI.Page
    {

        protected void Page_Load(object sender, System.EventArgs e)
        {

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "act_dbcrlist.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                
                labelcompany.Text = PrmComp;
                Session["Customers_Company"] = labelcompany.Text;

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

                if (Session["User"] != null)
                {
                    lblUser.Text = UserLogin.UserName;

                    if (Session["customer_dbcr_posted"] == null)
                        Session["customer_dbcr_posted"] = CheckBox1.Checked;
                    else
                        CheckBox1.Checked = Convert.ToBoolean(Session["customer_dbcr_posted"]);                  

                }


            } //  ! Page.IsPostBack
            
            try
            {
                if (Session["debit_memo_cust_index1"] == null)
                {
                    GridView1.SelectedIndex = 0;

                    Session["debit_memo_cust_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;


                }
            }
            catch
            {

            }
            try
            {
                if (Session["debit_memo_cust_index1"] != null)
                {
                    GridView1.SelectedIndex = Convert.ToInt32(Session["debit_memo_cust_index1"]);

                    Session["debit_memo_cust_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;


                }
            }
            catch
            {

            }
            Session["Rowuser"] = UserLogin.UserName;

            try
            {
                TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
                //ddl_display.Text = Convert.ToString(Session["gridsize"]);
                Session["size"] = Convert.ToInt32(ddl_display.Text);
                GridView1.PageSize = Convert.ToInt32(Session["size"]);
            }
            catch
            {

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



        protected void ddl_display_TextChanged(object sender, EventArgs e)
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            Session["gridsize"] = ddl_display.Text;
            ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

        }


        protected void GridView1_SelectedIndex(object sender, EventArgs e)
        {
            Session["debit_memo_cust_index1"] = GridView1.SelectedIndex;

            Session["debit_memo_cust_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;


        }

        protected void btnSearch_Click(object sender, EventArgs e)
        {
            Session["debit_memo_cust"] = customer_TextBox.Text;
            Session["customer_dbcr_posted"] = CheckBox1.Checked;            
            
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
            ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = customer_TextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmout"].DefaultValue = Convert.ToString(Session["customer_dbcr_posted"]);

            Session["debit_memo_cust_index1"] = null;
        }
        protected void btnShowAll_Click(object sender, EventArgs e)
        {
            customer_TextBox.Text = "";
            CheckBox1.Checked = false;

            Session["debit_memo_cust"] = customer_TextBox.Text;
            Session["customer_dbcr_posted"] = CheckBox1.Checked;            

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
            ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = customer_TextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmout"].DefaultValue = Convert.ToString(Session["customer_dbcr_posted"]);

            Session["debit_memo_cust_index1"] = null;
        }
        protected void lnk_viewcustomers_Click(object sender, EventArgs e)
        {
            Response.Redirect("view_act_dbcr.aspx");
        }

        protected void img_btn_exit_click(object sender, EventArgs e)
        {
            string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
            if (sLoginURL == "")
            {
                Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
                return;
            }

            Page.Session.Clear();
            if (Request.Cookies["showmenu"] != null)
            {
                Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
            }
            Response.Redirect(sLoginURL);
        }

        protected void img_btn_add_click(object sender, EventArgs e)
        {
            Session["custdebit_memo_add_button"] = "add";
            Response.Redirect("view_act_dbcr.aspx");

        }


    }

